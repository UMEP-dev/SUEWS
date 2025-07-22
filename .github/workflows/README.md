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
- Automatic model selection (Sonnet for normal tasks, Opus for complex ones)
- Full access to SUEWS development tools (make dev, make test, etc.)
- Project-specific instructions from CLAUDE.md
- Code review capabilities integrated
- Post-processing with reactions and PR linking

### 2. Format PR (`format-pr.yml`)
Simplified PR-based formatting workflow that:
- Runs on pull requests when code changes are detected
- Can be manually triggered via Actions tab for any PR
- Formats Python code using ruff v0.8.6
- Formats Fortran code using fprettify v0.3.7
- Commits directly to the PR branch
- Posts a comment notifying about formatting changes
- Skips forked repositories automatically
- No permission issues or workflow chains

**Manual Trigger:**
1. Go to Actions tab → Format PR workflow
2. Click "Run workflow"
3. Enter the PR number to format
4. Click "Run workflow" button

### 3. Build and Publish (`build-publish_to_pypi.yml`)
Automated build and publish workflow that:
- Builds wheels for multiple platforms (Linux, macOS, Windows)
- Supports Python 3.9-3.13
- Runs tests on each platform
- Publishes to TestPyPI on every push
- Publishes to PyPI on tagged releases
- Skips builds for auto-format commits (via `[skip ci]`)

### 4. Debug cibuildwheel (`cibuildwheel-debug.yml`)
Interactive debugging environment for cibuildwheel build issues with SSH access, Claude Code CLI integration, and comprehensive error capture.

**Key Features:**
- Manual triggering with validated platform/architecture combinations
- SSH sessions at multiple stages (before-build, after-failure, always)
- AI-assisted debugging with Claude Code CLI
- Detailed logging and artifact collection

**Quick Start:**
Actions tab → "Debug cibuildwheel with SSH" → Run workflow → Select platform-arch combination

> 📋 **Full documentation and usage examples** are available in the workflow file comments at the top of `cibuildwheel-debug.yml`

## Disabled Workflows

The following workflows have been disabled to prevent conflicts:
- `auto-format.yml.disabled` - Previously formatted master branch and created PRs
- `fprettify.yml.disabled` - Previously ran on all branches except master
- `ruff-format.yml.disabled` - Previously ran on PRs and non-master branches
- `check-master-files.yml.disabled` - Legacy workflow

These have been replaced by the unified `auto-format.yml` workflow that runs only on master.

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

2. **Security:**
   - Keep API keys secure in GitHub Secrets
   - Only add trusted users to CLAUDE_AUTHORIZED_USERS
   - Review Claude's code changes carefully
   - Regularly audit the authorised users list

3. **Performance:**
   - Claude workflows consume GitHub Actions minutes
   - Use specific commands to reduce token usage
   - Set appropriate turn limits for your use case

## Troubleshooting

- If Claude doesn't respond, check if you're in the authorised users list
- For build failures, check the tmate debugging session (15 min timeout)
- For auto-formatting issues, check the workflow logs and PR creation status
- If formatting PRs aren't created, ensure the workflow has pull-request write permissions