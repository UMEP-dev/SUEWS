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
- Builds wheels for multiple platforms (Linux, macOS, Windows)
- Supports Python 3.9-3.13
- Runs tests on each platform
- Publishes to TestPyPI on every push
- Publishes to PyPI on tagged releases

**Recent Improvements:**
- Added concurrency control to prevent conflicting builds
- Enhanced caching for faster builds
- Improved security with proper permissions
- Added wheel validation before publishing
- Updated to latest action versions
- **Fixed duplicate runs**: Push events only trigger on master/main branches and tags, PR events handle feature branch testing

### 3. Fortran Prettify (`fprettify.yml`)
Automatically formats Fortran code using fprettify when:
- Changes are pushed to any branch except master
- Fortran source files are modified

**Recent Improvements:**
- Added concurrency control
- Smart formatting detection (only commits when needed)
- Improved caching for faster execution
- Better commit messages
- **Fixed duplicate runs**: Runs on feature branch pushes and PR events without duplication

### 4. Claude Code Review (`claude-code-review.yml`)
Automated code review using Claude for pull requests:
- Structured review format with collapsible sections
- Focuses on code quality, security, and best practices
- Uses Claude Sonnet 4 for cost-effective reviews
- Sticky comments that update on PR changes

### 5. Continuous Integration (`ci.yml`)
Comprehensive CI pipeline for code quality:
- Python linting with ruff
- Code formatting checks
- Branch protection for master/main
- Proper caching and performance optimization
- Detailed job summaries

### 6. Python Auto-Format (`ruff-format.yml.disabled`)
**Status: Disabled** - Enhanced Python formatting workflow:
- Automatic code formatting with ruff
- Smart detection to avoid unnecessary commits
- Comprehensive linting after formatting
- Can be enabled by removing `.disabled` extension

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

4. **Code Quality:**
   - New CI workflow ensures consistent code quality
   - Automatic formatting available for both Python and Fortran
   - Branch protection prevents direct pushes to master
   - Comprehensive testing and validation

## Troubleshooting

- If Claude doesn't respond, check if you're in the authorised users list
- For build failures, check the tmate debugging session (15 min timeout)
- For fprettify issues, ensure your PAT has write permissions