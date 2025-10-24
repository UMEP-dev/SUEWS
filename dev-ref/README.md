# SUEWS Developer Documentation

This directory contains internal documentation for SUEWS core developers.

## Quick Navigation

### Getting Started
- **[Onboarding Guide](onboarding-guide.md)** - Comprehensive guide for new developers
  - Environment setup, workflow overview, first contribution checklist
  - Start here if you're new to the team
  
- **[Building Locally](building-locally.md)** - Quick guide to build and test SUEWS from source
  - Prerequisites, setup with pip or conda/mamba, and test commands

### Development Guidelines
- **[Coding Guidelines](CODING_GUIDELINES.md)** - Code style and conventions
  - Python and Fortran standards
  - Naming conventions and best practices
  
- **[Review Process](REVIEW_PROCESS.md)** - Pull request review procedures
  - How we review code
  - What to look for in reviews

### Testing
- **[Testing Guidelines](testing/TESTING_GUIDELINES.md)** - Overall testing strategy
  - Test design principles
  - FIRST principles and AAA pattern
  
- **[Fortran Test Patterns](testing/FORTRAN_TEST_PATTERNS.md)** - Fortran-specific testing
  - Available modules via f90wrap
  - Common test patterns
  
- **[Error Handling](testing/ERROR_HANDLING_PATTERNS.md)** - Error handling best practices
  - Dual-layer error strategy
  - User vs developer error messages

### Operations
- **[Issue Triage](ISSUE_TRIAGE.md)** - How to manage GitHub issues
  - Priority levels and labels
  - Assignment and workflow
  
- **[Release Manual](RELEASE_MANUAL.md)** - Release procedures
  - Version numbering
  - Release checklist
  - Distribution to PyPI

## External Links
- [GitHub Repository](https://github.com/UMEP-dev/SUEWS)
- [User Documentation](https://suews.readthedocs.io)
- [GitHub Issues](https://github.com/UMEP-dev/SUEWS/issues)
- [GitHub Discussions](https://github.com/UMEP-dev/SUEWS/discussions)

## Note for AI Assistants
AI-specific instructions and workflows are maintained separately in `.claude/`. See `.claude/CLAUDE.md` for AI assistant guidance.
