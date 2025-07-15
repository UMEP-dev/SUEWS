# GitHub Actions Workflows for SUEWS

This directory contains GitHub Actions workflows for the SUEWS project.

## Available Workflows

### 1. Claude Code (`claude.yml`)
AI-powered workflow automation using Claude. Mention `@claude` to:
- Convert issues to pull requests
- Get implementation help
- Fix bugs automatically
- Code assistance with project context
- Code review and security analysis

**Usage Examples:**
```
# In an issue
@claude please implement this feature

# In a PR comment
@claude help me fix the failing tests

# Code review
@claude review this PR for security vulnerabilities
@claude check if this follows our coding standards

# For complex tasks
@claude complex: refactor the entire validation system
```

**Features:**
- Uses Claude Sonnet 4 for optimal performance and cost efficiency
- Full access to SUEWS development tools (make dev, make test, etc.)
- Project-specific instructions from CLAUDE.md
- Code review capabilities integrated
- Enhanced error handling and user feedback
- Concurrency protection to prevent conflicts

### 2. Build and Publish (`build-publish_to_pypi.yml`)
Automated build and publish workflow that:
- **Builds wheels on ALL branches** for comprehensive testing
- Supports Python 3.9-3.13 on multiple platforms (Linux, macOS, Windows)
- Runs tests on each platform to ensure quality
- Publishes to TestPyPI on every push (not PRs)
- Publishes to PyPI on release tags (excludes dev tags)

**Recent Improvements:**
- Added concurrency control to prevent conflicting builds
- Enhanced caching for faster builds
- Improved security with proper permissions
- Added wheel validation before publishing
- Updated to latest action versions
- **Comprehensive testing**: Builds run on all branches to catch issues early
- **Smart publishing**: Only publishes on push events, not PR events
- **Correct tag format**: Uses repository's date-based tag format (e.g., 2025.1.1)
- **🤖 Automatic fixing**: Claude Code automatically intervenes on build failures
- **Fallback debugging**: Manual tmate sessions available if Claude cannot fix issues

#### 🤖 Automatic Build Failure Resolution
When a build fails, the workflow automatically:
1. **Checks user authorization** for Claude Code access
2. **Triggers Claude Code directly** with detailed failure context
3. **Provides comprehensive analysis prompt** with specific focus areas
4. **Falls back to manual debugging** if Claude cannot resolve the issue

**Failure types automatically handled:**
- Build wheel failures (platform-specific issues)
- Deployment failures (wheel validation, PyPI publishing)
- Dependencies and environment issues
- Compilation errors
- Fortran compilation problems

**Features:**
- **Direct integration**: Immediate Claude Code response without polling
- **Authorized users only**: Respects existing Claude Code security settings
- **Context-aware**: Provides specific failure type and platform information
- **Comprehensive tooling**: Full access to development tools for fixing
- **Transparency tracking**: Creates issues to document what was fixed (when successful)
- **Audit trail**: Full record of automatic fixes for review and accountability

### 3. Auto-Format (`auto-format.yml`)
**Manual formatting workflow** that can be triggered when needed:
- Formats Python code with ruff
- Formats Fortran code with fprettify
- Only commits when changes are detected
- Triggered via workflow_dispatch (manual)

**Features:**
- Selective formatting (choose Python and/or Fortran)
- Smart change detection
- Proper commit messages
- Job summaries showing results

### 4. Claude Code Review (`claude-code-review.yml`)
Automated code review using Claude for pull requests:
- Structured review format with collapsible sections
- Focuses on code quality, security, and best practices
- Uses Claude Sonnet 4 for cost-effective reviews
- Sticky comments that update on PR changes

### 5. Continuous Integration (`ci.yml`)
Comprehensive CI pipeline for code quality:
- Python linting with ruff
- Python formatting checks
- Fortran formatting checks
- Branch protection for master/main
- Proper caching and performance optimization
- Detailed job summaries

**Recent Improvements:**
- **Integrated formatting checks**: Now includes both Python and Fortran formatting validation
- **Centralized quality control**: All code quality checks in one place
- **Efficient execution**: Only runs on push to master/main and PR events

### 6. Disabled Workflows
**Status: Disabled** - Legacy formatting workflows replaced by integrated CI:
- `fprettify.yml.disabled` - Standalone Fortran formatting
- `ruff-format.yml.disabled` - Standalone Python formatting

These have been replaced by the integrated CI workflow for better efficiency.

## Configuration

### Required Secrets
- `ANTHROPIC_API_KEY`: Your Anthropic API key for Claude
- `CLAUDE_AUTHORIZED_USERS`: **REQUIRED** - List of authorised GitHub usernames
  - Format option 1 (comma-separated): `sunt05,user2,user3`
  - Format option 2 (newline-separated):
    ```
    sunt05
    user2
    user3
    ```
- `PYPI_API_TOKEN`: PyPI token for publishing releases
- `TEST_PYPI_API_TOKEN`: TestPyPI token for test publishing
- `PAT`: Personal Access Token for fprettify commits


## Security Configuration

**IMPORTANT**: Claude workflows enforce strict access control. You MUST configure the `CLAUDE_AUTHORIZED_USERS` secret with a list of GitHub usernames who are allowed to trigger Claude.

Example configurations in repository settings:

**Option 1 - Comma-separated (recommended for small lists):**
```
sunt05,trusted-user2,trusted-user3
```

**Option 2 - Newline-separated (recommended for longer lists):**
```
sunt05
trusted-user2
trusted-user3
trusted-user4
```

Without this configuration, all Claude requests will be denied.

## Best Practices

1. **Using Claude:**
   - Be specific in your requests
   - Use "complex" keyword for tasks requiring more computational power
   - Check Claude's suggestions before merging
   - Claude now uses Sonnet 4 for better cost efficiency

2. **Security:**
   - Keep API keys secure in GitHub Secrets
   - Only add trusted users to CLAUDE_AUTHORIZED_USERS
   - Review Claude's code changes carefully
   - Regularly audit the authorised users list
   - All workflows now use proper permission restrictions

3. **Performance:**
   - All workflows now include concurrency control
   - Extensive caching reduces build times
   - Smart detection prevents unnecessary work
   - Workflows only run when relevant files change
   - **Integrated formatting**: Checks moved to CI workflow for efficiency

4. **Code Quality:**
   - Integrated CI workflow with all quality checks
   - Formatting validation for both Python and Fortran in CI
   - Manual formatting workflow available when needed
   - Branch protection prevents direct pushes to master
   - Comprehensive testing and validation

## Troubleshooting

- If Claude doesn't respond, check if you're in the authorised users list
- For build failures, check the tmate debugging session (15 min timeout)
- For fprettify issues, ensure your PAT has write permissions