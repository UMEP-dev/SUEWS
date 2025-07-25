# Design: Add Changelog to Documentation Site

## Architecture Overview

The implementation uses Sphinx's built-in `include` directive to incorporate the CHANGELOG.md file into the documentation. This approach ensures the changelog remains a single source of truth while being accessible through the documentation site.

## Technical Approach

### 1. File Structure

```
docs/source/
├── index.rst                    # Main documentation index (to be modified)
├── changelog.rst               # New file that includes CHANGELOG.md
└── recent-development/
    └── index.rst               # Developer section index (to be modified)
```

### 2. Changelog Integration Strategy

#### Option Selected: Direct Include with MyST Parser

**Rationale**: 
- Maintains single source of truth (CHANGELOG.md)
- No duplication or synchronisation issues
- Automatic updates when CHANGELOG.md changes
- Preserves all markdown formatting and links

**Implementation**:
```rst
# changelog.rst
Changelog
=========

.. include:: ../../CHANGELOG.md
   :parser: myst_parser.sphinx_
```

### 3. Navigation Integration

#### Primary Location
- Add to `recent-development/index.rst` under "For developers/contributors"
- Position after "Recent Updates" section for logical flow

#### Secondary Reference
- Add prominent link in main `index.rst` 
- Position in "Quick Links" or similar section for visibility

### 4. Build Configuration

#### Prerequisites
- MyST parser must be enabled in Sphinx configuration
- Verify `myst_parser` is in installed extensions

#### Configuration Updates
```python
# docs/source/conf.py
extensions = [
    ...
    'myst_parser',  # Should already be present
    ...
]

# Ensure markdown support is configured
source_suffix = {
    '.rst': 'restructuredtext',
    '.md': 'markdown',
}
```

### 5. Rendering Considerations

#### Markdown Compatibility
- CHANGELOG.md uses standard markdown syntax
- Version headers (## v7.0.0) will render as subsections
- Links to issues/PRs will be preserved
- Code blocks and lists will render correctly

#### Table of Contents
- Changelog will appear in documentation TOC
- Version headers create navigable subsections
- Users can jump to specific versions

### 6. Error Handling

#### Missing File Handling
```rst
.. only:: not fileexists('../../CHANGELOG.md')

   .. warning::
      Changelog file not found. Please ensure CHANGELOG.md exists in the repository root.
```

#### Build Verification
- Add build-time check for CHANGELOG.md existence
- Fail gracefully with informative error message

## Implementation Details

### File: `docs/source/changelog.rst`

```rst
:orphan:

=========
Changelog
=========

This page contains the complete history of changes for SUEWS.

.. include:: ../../CHANGELOG.md
   :parser: myst_parser.sphinx_
```

### File: `docs/source/index.rst` (modification)

Add to appropriate section:
```rst
**Quick Links**

* :doc:`changelog` - View recent changes and version history
* :doc:`recent-development/index` - Information for developers
```

### File: `docs/source/recent-development/index.rst` (modification)

Add to TOC tree:
```rst
.. toctree::
   :maxdepth: 2
   :numbered:

   recent-updates
   ../changelog
   ... (other existing entries)
```

## Testing Strategy

### Build Testing
1. Run full documentation build locally
2. Verify no errors or warnings related to changelog
3. Check build time hasn't significantly increased

### Content Verification
1. Verify changelog appears in navigation
2. Check all markdown elements render correctly
3. Test all links remain functional
4. Confirm version headers are navigable

### Cross-platform Testing
1. Test on Linux (CI environment)
2. Test on macOS (developer environment)
3. Verify Windows compatibility if applicable

## Automated Changelog Management

### Overview

Two complementary approaches for automating changelog updates:

1. **Approach A: Claude Bot Automation** - AI-powered changelog generation
2. **Approach B: PR-Enforced Updates** - Require changelog in every PR

### Approach A: Claude Bot Automation

**Note**: The project already has Claude bot integration via `.github/workflows/claude.yml` that responds to @claude mentions in issues and PRs. This existing integration can be extended for changelog automation.

#### Architecture
```yaml
# .github/workflows/changelog-automation.yml
name: Automated Changelog Update

on:
  pull_request:
    types: [closed]
    branches: [master]

jobs:
  update-changelog:
    if: github.event.pull_request.merged == true
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - name: Generate Changelog Entry
        uses: ./.github/actions/claude-changelog
        with:
          pr_number: ${{ github.event.pull_request.number }}
          pr_title: ${{ github.event.pull_request.title }}
          pr_body: ${{ github.event.pull_request.body }}
      - name: Create PR with Changelog Update
        uses: peter-evans/create-pull-request@v5
```

#### Claude Integration Design
```python
# .github/actions/claude-changelog/main.py
def generate_changelog_entry(pr_data):
    """Generate changelog entry using Claude API"""
    prompt = f"""
    Analyse this PR and generate a changelog entry:
    Title: {pr_data['title']}
    Description: {pr_data['body']}
    Files changed: {pr_data['files_changed']}
    
    Follow the existing CHANGELOG.md format:
    - Use categories: Added, Changed, Fixed, Removed
    - Be concise but descriptive
    - Include PR number as [#{pr_data['number']}]
    """
    
    # Call Claude API
    entry = claude.generate(prompt)
    return format_changelog_entry(entry, pr_data['number'])
```

#### Changelog Entry Format
```markdown
## [Unreleased]

### Added
- Feature description [#123](https://github.com/UMEP-dev/SUEWS/pull/123)

### Changed
- Change description [#124](https://github.com/UMEP-dev/SUEWS/pull/124)
```

### Approach B: PR-Enforced Updates

#### Branch Protection Rules
```yaml
# GitHub branch protection settings
- Require status checks to pass:
  - changelog-check
- Require branches to be up to date
```

#### Validation Workflow
```yaml
# .github/workflows/changelog-check.yml
name: Changelog Check

on:
  pull_request:
    types: [opened, synchronize]

jobs:
  check-changelog:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - name: Check Changelog Modified
        id: changelog_check
        run: |
          if git diff --name-only origin/master..HEAD | grep -q "CHANGELOG.md"; then
            echo "✅ Changelog updated"
            exit 0
          else
            # Check if PR is exempt (docs-only, CI-only)
            if [[ "${{ github.event.pull_request.labels }}" =~ "skip-changelog" ]]; then
              echo "ℹ️ Changelog skip allowed"
              exit 0
            fi
            echo "❌ Please update CHANGELOG.md"
            exit 1
          fi
```

### Hybrid Approach (Recommended)

Combine both approaches for maximum flexibility:

1. **Default**: Require manual changelog updates in PRs
2. **Fallback**: Use Claude bot for:
   - Emergency fixes
   - Automated dependency updates
   - PRs from new contributors

```yaml
# Enhanced workflow combining both approaches
name: Changelog Management

on:
  pull_request:
    types: [opened, synchronize, closed]

jobs:
  check-changelog:
    if: github.event.action != 'closed'
    # Run validation check
    
  auto-update-changelog:
    if: |
      github.event.action == 'closed' &&
      github.event.pull_request.merged == true &&
      !contains(github.event.pull_request.files, 'CHANGELOG.md')
    # Run Claude automation
```

### Conventional Commits Integration

#### Commit Message Parsing
```python
def parse_conventional_commit(message):
    """Parse conventional commit format"""
    patterns = {
        'feat': 'Added',
        'fix': 'Fixed',
        'docs': 'Documentation',
        'style': 'Changed',
        'refactor': 'Changed',
        'test': 'Testing',
        'chore': 'Maintenance'
    }
    
    match = re.match(r'^(\w+)(?:\(([^)]+)\))?: (.+)$', message)
    if match:
        type, scope, description = match.groups()
        category = patterns.get(type, 'Changed')
        return category, description
    return None, None
```

### Release Process Integration

#### Version Bumping
```python
def prepare_release_changelog(version):
    """Update changelog for release"""
    today = datetime.now().strftime('%Y-%m-%d')
    
    # Replace [Unreleased] with version
    changelog = changelog.replace(
        '## [Unreleased]',
        f'## [Unreleased]\n\n## [{version}] - {today}'
    )
    
    # Add comparison link
    changelog += f'\n[{version}]: https://github.com/UMEP-dev/SUEWS/compare/v{prev_version}...v{version}'
    
    return changelog
```

### Documentation Auto-Update

Since documentation already uses `.. include:: ../../CHANGELOG.md`, no changes needed. The workflow:

1. PR merged → Changelog updated
2. Documentation build triggered
3. Changes automatically reflected

### Conflict Resolution Strategy

#### Merge Queue Integration
```yaml
# Use GitHub merge queue to serialize changelog updates
merge_group:
  checks:
    - changelog-check
    - build-docs
```

#### Alternative: Changelog Fragments
```
changelog.d/
├── 123.added.md    # PR #123 additions
├── 124.fixed.md    # PR #124 fixes
└── template.md     # Template for contributors
```

Build changelog from fragments during release.

## Tag System Design

### Overview

To better distinguish between user-facing and developer changes, implement a revised tagging system that clearly indicates the audience for each change.

### Proposed Tag Structure

#### Primary Tags (Audience-Focused)
- **[user]**: Changes visible to end users of SUEWS/SuPy
  - New simulation features
  - Changes to model behaviour
  - API changes
  - Bug fixes affecting results
  - Breaking changes
  
- **[dev]**: Changes affecting developers/contributors only
  - Build system improvements
  - Testing infrastructure
  - Development workflows
  - Internal refactoring
  - Documentation for developers

#### Secondary Tags (Change Type)
These can be combined with primary tags:
- **[new]**: New features or capabilities
- **[fix]**: Bug fixes
- **[change]**: Modifications to existing behaviour
- **[remove]**: Deprecated features removed
- **[docs]**: Documentation updates

#### Deprecated Tags (Backward Compatibility)
Keep for historical entries but discourage new use:
- **[feature]** → Use [user][new] or [dev][new]
- **[bugfix]** → Use [user][fix] or [dev][fix]
- **[maintenance]** → Use [dev][change]
- **[doc]** → Use [user][docs] or [dev][docs]
- **[change]** → Use [user][change]

### Implementation Approach

#### 1. Tag Format in CHANGELOG.md
```markdown
- 02 Jul 2025:
  - [user][new] Added automatic annotated YAML generation for parameter validation errors
  - [user][fix] Fixed parameter validation false positives (#448)
  - [dev][new] Added automated worktree management scripts
```

#### 2. Filtered Documentation Views

Create two changelog views in the documentation:

**User Changelog** (`changelog-user.rst`):
```rst
User Changelog
==============

This changelog shows changes that affect SUEWS/SuPy users.

.. include:: ../../CHANGELOG.md
   :parser: myst_parser.sphinx_
   :start-after: <!-- BEGIN-USER-CHANGELOG -->
   :end-before: <!-- END-USER-CHANGELOG -->
```

**Full Changelog** (`changelog.rst`):
```rst
Full Changelog
==============

This changelog includes all changes, including developer-focused improvements.

.. include:: ../../CHANGELOG.md
   :parser: myst_parser.sphinx_
```

#### 3. Changelog Processing Script

Create a Python script to generate filtered views:

```python
# scripts/process_changelog.py
def filter_changelog(input_file, output_file, tags_to_include):
    """Filter changelog entries by tags"""
    with open(input_file, 'r') as f:
        lines = f.readlines()
    
    filtered_lines = []
    include_entry = False
    
    for line in lines:
        if line.strip().startswith('-') and '[' in line:
            # Check if any included tag is present
            include_entry = any(tag in line for tag in tags_to_include)
        
        if include_entry or not line.strip().startswith('-'):
            filtered_lines.append(line)
    
    with open(output_file, 'w') as f:
        f.writelines(filtered_lines)
```

#### 4. Documentation Build Integration

Modify the Sphinx build process to generate filtered views:

```python
# docs/source/conf.py
def setup(app):
    """Custom setup for changelog filtering"""
    app.connect('builder-inited', process_changelog)

def process_changelog(app):
    """Generate filtered changelog views before build"""
    import subprocess
    subprocess.run([
        'python', 'scripts/process_changelog.py',
        '--input', '../../CHANGELOG.md',
        '--user-output', 'source/_generated/changelog-user.md',
        '--tags', '[user]'
    ])
```

### Tag Assignment Guidelines

#### For Contributors

Add to CONTRIBUTING.md:

```markdown
## Changelog Tags

When adding changelog entries, use these tags:

### Primary Tags (Required)
- **[user]**: Changes visible to SUEWS/SuPy users
  - New simulation features
  - Model behaviour changes
  - API modifications
  - Result-affecting bug fixes
  
- **[dev]**: Developer/contributor changes only
  - Build system (Makefile, meson)
  - Testing infrastructure
  - Development tools (Docker, worktrees)
  - Internal refactoring

### Secondary Tags (Optional but Recommended)
- **[new]**: New features
- **[fix]**: Bug fixes
- **[change]**: Behaviour modifications
- **[remove]**: Deprecated removals
- **[docs]**: Documentation

### Examples
- `[user][new]`: New SUEWS physics option
- `[user][fix]`: Fixed energy balance calculation
- `[dev][new]`: Added Docker development environment
- `[dev][fix]`: Fixed test suite race condition
```

#### For Automated Systems

Claude bot tag assignment logic:
```python
def determine_tags(pr_data):
    """Intelligently assign tags based on PR content"""
    tags = []
    
    # Check files changed
    user_paths = ['src/supy/', 'src/suews/', 'examples/']
    dev_paths = ['.github/', 'test/', 'docker/', '.claude/']
    
    files = pr_data['files_changed']
    if any(f.startswith(p) for f in files for p in user_paths):
        tags.append('[user]')
    elif any(f.startswith(p) for f in files for p in dev_paths):
        tags.append('[dev]')
    
    # Check PR labels
    if 'user-facing' in pr_data['labels']:
        tags.append('[user]')
    if 'infrastructure' in pr_data['labels']:
        tags.append('[dev]')
    
    # Determine change type
    if 'feat' in pr_data['title'].lower():
        tags.append('[new]')
    elif 'fix' in pr_data['title'].lower():
        tags.append('[fix]')
    
    return tags
```

### Migration Strategy

1. **Phase 1**: Update contributing docs with new tag system
2. **Phase 2**: Start using new tags for new entries
3. **Phase 3**: Implement filtered views in documentation
4. **Phase 4**: Optionally migrate historical entries in batches

## Future Considerations

### Potential Enhancements
1. **Changelog excerpts**: Show recent changes on main page
2. **Version filtering**: Allow viewing changes for specific versions
3. **Search integration**: Ensure changelog content is searchable
4. **Release notes**: Separate detailed release notes from changelog
5. **Changelog analytics**: Track most active contributors
6. **Breaking changes detection**: Highlight breaking changes prominently
7. **Interactive filtering**: Allow users to dynamically filter by tags in the web UI

### Maintenance
1. Regular review of automated entries for quality
2. Periodic changelog cleanup and formatting
3. Monitor for markdown syntax that may not render well
4. Consider changelog size limits if it grows very large
5. Update automation rules based on team feedback

## Security Considerations

- No security implications as changelog is already public
- No user input or dynamic content
- Standard Sphinx security practices apply

## Performance Impact

- Minimal impact on build time (single file inclusion)
- No runtime performance impact (static HTML)
- Changelog size (~30KB) is negligible for modern systems