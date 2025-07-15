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

### 2. Build and Publish (`build-publish_to_pypi.yml`)
Automated build and publish workflow that:
- Builds wheels for multiple platforms (Linux, macOS, Windows)
- Supports Python 3.9-3.13
- Runs tests on each platform
- Publishes to TestPyPI on every push
- Publishes to PyPI on tagged releases

### 3. Fortran Prettify (`fprettify.yml`)
Automatically formats Fortran code using fprettify when:
- Changes are pushed to any branch except master
- Fortran source files are modified

### 4. Debug cibuildwheel with SSH (`cibuildwheel-debug.yml`)
Interactive debugging environment for cibuildwheel build issues:
- **Manual triggering** with specific platform/Python/architecture combinations
- **SSH access** at different build stages (before, after failure, or both)
- **Claude Code CLI** pre-installed for AI-assisted debugging
- **Comprehensive logging** with build artifacts and system information

**Usage:**
1. Go to Actions → "Debug cibuildwheel with SSH"
2. Select parameters:
   - Platform: ubuntu-latest, macos-13, macos-latest, windows-2025
   - Python: cp39, cp310, cp311, cp312, cp313
   - Architecture: x86_64, arm64, AMD64, x86
   - Debug mode: before-build, after-failure, always, disabled
3. Connect via SSH when prompted
4. Use `claude -p "help debug this error"` for assistance

**Security:** SSH access restricted to workflow actor only

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
- For fprettify issues, ensure your PAT has write permissions