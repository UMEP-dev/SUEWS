# Feature: Fast Development Build System

## Context
This branch focuses on creating a fast development build system and comprehensive documentation overhaul. The goal is to improve developer productivity by reducing build times and enhancing documentation quality and accessibility.

## GitHub Issues to Address
- **#411**: Clean-up and content revision of the 2025 docs system - CLOSED but active work
- **#343**: Update docs site - CLOSED but ongoing
- **#365**: Add an interactive editor for SUEWS config - CLOSED but enhance
- **#328**: v2025 release - to check (release-checklist)

## Progress Tracking
- [x] Documentation structure overhaul
  - [x] Forcing data preparation guide
  - [x] Enhanced component documentation
  - [x] Integration of output file documentation
  - [x] Configuration builder improvements
- [ ] Fast build system implementation
  - [ ] Create development-only build targets
  - [ ] Implement incremental compilation
  - [ ] Add build caching system
  - [ ] Create quick-test targets
- [ ] Documentation enhancements (ongoing)
  - [ ] Complete API documentation
  - [ ] Add interactive examples
  - [ ] Improve search functionality
  - [ ] Add version switcher
- [ ] Developer tooling
  - [ ] Hot-reload for documentation
  - [ ] Automated API doc generation
  - [ ] Build performance profiling
  - [ ] Development environment setup scripts

## Key Decisions
- Separate development and production build paths
- Use modern build tools for faster compilation
- Implement proper dependency tracking
- Focus on iterative development workflow

## Implementation Notes
- This branch already has significant documentation work
- Build system improvements should complement doc work
- Consider using ccache for Fortran compilation
- Implement parallel builds where possible
- Documentation should auto-build on changes
- **[2025-06-27 Update]**: Demonstrated plan update workflow from worktree
- **Note**: Plans are accessible via `../../.claude-plans/` from worktree directories
- **Session Note**: Added workflow documentation for Claude Code plan updates

## Files to Modify
- `Makefile` - Add fast development targets
- `meson.build` - Optimise build configuration
- `docs/Makefile` - Add watch/hot-reload targets
- `docs/source/conf.py` - Documentation enhancements
- `.github/workflows/` - CI optimisations
- Development setup scripts (new)

## Build Performance Goals
1. < 5 second incremental builds
2. < 30 second full rebuild
3. < 2 second documentation preview
4. Parallel test execution
5. Cached dependency builds

## Documentation Goals
1. Comprehensive API reference
2. Interactive configuration examples
3. Video tutorials integration
4. Improved navigation and search
5. Mobile-responsive design

## Current Status
This branch is actively developed with extensive documentation improvements already in place. Focus should be on:
1. Completing the documentation overhaul
2. Implementing the fast build system
3. Preparing for v2025 release