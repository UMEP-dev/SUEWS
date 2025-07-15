# Feature: cibuildwheel SSH Debug Workflow

## Lead Developer
- **GitHub**: @sunt05
- **Started**: 2025-07-14

## Context
Create a dispatch workflow that allows manual triggering with specific platform and Python version combinations, includes SSH access for debugging, and has Claude Code CLI installed for AI-assisted debugging of cibuildwheel build errors.

## GitHub Issues
- None (internal debugging tool)

## Status
- **Current**: doing
- **Outcome**: pending
- **Completed**: [pending]
- **PR**: [pending]

## Progress Tracking

### Phase 1: Initial Setup
- [x] Create worktree for feature
- [x] Create dispatch workflow with platform/python selection

### Phase 2: Enhancement
- [ ] Add comprehensive logging and error capture
- [ ] Test workflow on different platforms
- [ ] Document usage instructions

## Key Decisions
- Use workflow_dispatch for manual triggering
- Include Claude Code CLI for AI-assisted debugging
- Provide SSH access at different stages of build process
- Support all major platforms and Python versions

## Key Features
1. **Manual Dispatch**: Workflow can be triggered manually with specific parameters
2. **Platform Selection**: Choose from ubuntu-latest, macos-13, macos-latest, windows-2025
3. **Python Version**: Select from cp39, cp310, cp311, cp312, cp313
4. **Architecture**: Choose x86_64, arm64, AMD64, or x86
5. **Debug Modes**:
   - `before-build`: SSH session before cibuildwheel starts
   - `after-failure`: SSH session only if build fails
   - `always`: SSH sessions both before and after
   - `disabled`: No SSH sessions
6. **Claude Code Integration**: CLI installed for AI-assisted debugging

## Implementation Notes
- Uses same environment variables as main build workflow
- Includes verbose logging (CIBW_BUILD_VERBOSITY: 3)
- Uploads logs and artifacts for post-mortem analysis
- SSH sessions limited to workflow actor for security
- 60-minute timeout for debug sessions

## Files to Modify
- `.github/workflows/cibuildwheel-debug.yml` (create)
- `.github/workflows/README-cibuildwheel-debug.md` (create)
- `.claude/plans/doing/feature-cibuildwheel-debug.md` (create)

## Testing Strategy
- Test workflow on all supported platforms
- Verify SSH access works correctly in each debug mode
- Test Claude Code CLI integration
- Validate log and artifact uploads

## Documentation Updates
- Create README for the debug workflow
- Document usage instructions in workflow file
- Add troubleshooting guide for common issues

## Usage Instructions
1. Go to Actions tab in GitHub
2. Select "Debug cibuildwheel with SSH" workflow
3. Click "Run workflow"
4. Select platform, Python version, architecture, and debug mode
5. When SSH session starts, connect using provided command
6. Use Claude Code CLI for debugging: `claude -p "help me debug this cibuildwheel error"`